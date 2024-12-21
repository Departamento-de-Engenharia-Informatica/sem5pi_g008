import { Router } from 'express';
import allergy from './routes/allergyRoute';
import medicalConditions from './routes/medicalConditionRoute';
import medicalRecord from './routes/medicalRecordRoute';
import medicalRecordCondition from './routes/medicalRecordConditionRoute';

export default () => {
	const app = Router();

  allergy(app);
  
  medicalRecord(app);
  medicalRecordCondition(app);

  medicalConditions(app);
  
	return app
}
