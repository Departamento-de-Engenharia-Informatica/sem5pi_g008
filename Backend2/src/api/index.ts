import { Router } from 'express';
import allergy from './routes/allergyRoute';
import medicalRecord from './routes/medicalRecordRoute';

export default () => {
	const app = Router();

  allergy(app);
  medicalRecord(app);

	return app
}
