import { Router } from 'express';
import allergy from './routes/allergyRoute';
import medicalRecord from './routes/medicalRecordRoute';
import medicalRecordCondition from './routes/medicalRecordConditionRoute';

export default () => {
	const app = Router();

  allergy(app);
  medicalRecord(app);
  medicalRecordCondition(app);

	return app
}
