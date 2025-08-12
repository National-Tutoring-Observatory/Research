import { lambdaHandler } from './app.js';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

async function runLocal() {
    console.log('🚀 Starting local annotation quality check...');
    
    const event = {
        inputDir: path.join(__dirname, '../../localData/analysis'),
        outputDir: path.join(__dirname, '../../localData/output')
    };
    
    const context = {};
    
    try {
        const result = await lambdaHandler(event, context);
        console.log('✅ Quality check completed successfully!');
        console.log('📊 Summary:', JSON.parse(result.body));
    } catch (error) {
        console.error('❌ Error running quality check:', error);
        process.exit(1);
    }
}

// Run if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
    runLocal();
}
