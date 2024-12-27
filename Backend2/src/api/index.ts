import { Router } from 'express';
import allergy from './routes/allergyRoute';
import medicalConditions from './routes/medicalConditionRoute';
import medicalRecord from './routes/medicalRecordRoute';
import familyhistory from './routes/familyHistoryRoute';

export default () => {
	const app = Router();

  allergy(app);
  
  medicalRecord(app);
  
  medicalConditions(app);

  familyhistory(app);
  
	return app
}
